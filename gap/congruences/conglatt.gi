############################################################################
##
##  conglatt.gi
##  Copyright (C) 2016                                   Michael C. Torpey
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## This file contains functions for a poset of congruences.
##
## When the congruences of a semigroup are computed, they form a lattice with
## respect to containment.  The information about the congruences' positions in
## this lattice may be stored in an IsCongruencePoset object (a component object
## based on a record) and can be retrieved from this object using the methods in
## this file.
##
## The list of congruences in the poset is stored as an attribute
## CongruencesOfPoset.  The partial order of the poset is stored as a digraph,
## where an edge (i,j) is present if and only if congruence i is a subrelation
## of congruence j.  When a congruence poset is displayed, it appears to the
## user as the list of out-neighbours of that digraph.
##

InstallMethod(ViewObj,
"for a congruence poset",
[IsCongruencePoset],
function(poset)
  if DigraphNrVertices(poset) = 0 then
    Print("<empty congruence poset>");
  else
    Print("<poset of ", DigraphNrVertices(poset), " congruences over ");
    ViewObj(UnderlyingSemigroupOfCongruencePoset(poset));
    Print(">");
  fi;
end);

InstallMethod(PrintObj,
"for a congruence poset",
[IsCongruencePoset],
function(poset)
  Print("PosetOfCongruences( ", CongruencesOfPoset(poset), " )");
end);

InstallMethod(Size,
"for a congruence poset",
[IsCongruencePoset],
DigraphNrVertices);

SEMIGROUPS.PrincipalXCongruencePosetNC :=
  function(S,
           restriction,
           SemigroupXCongruence,
           GeneratingPairsOfXSemigroupCongruence)
  local report, pairs, total, congs, nrcongs, children, parents, last_collected,
        nr, pair, badcong, newchildren, newparents, newcong, i, pair1, c, p,
        poset;

  # Suppress reporting
  report := SEMIGROUPS.OptionsRec(S).report;
  SEMIGROUPS.OptionsRec(S).report := false;

  # Only try pairs from <restriction>
  pairs := IteratorOfCombinations(AsList(restriction), 2);
  total := Binomial(Size(restriction), 2);

  # Get all the unique principal congs
  Info(InfoSemigroups, 1, "Finding principal congruences . . .");
  congs := [];      # List of all congs found so far
  nrcongs := 0;     # Number of congs found so far
  children := [];   # List of lists of children
  parents := [];    # List of lists of parents
  last_collected := 0;
  nr := 0;
  for pair in pairs do
    badcong := false;
    newchildren := [];  # Children of newcong
    newparents := [];   # Parents of newcong
    newcong := SemigroupXCongruence(S, pair);
    for i in [1 .. Length(congs)] do
      pair1 := GeneratingPairsOfXSemigroupCongruence(congs[i])[1];
      if CongruenceTestMembershipNC(congs[i], pair[1], pair[2]) then
        if CongruenceTestMembershipNC(newcong, pair1[1], pair1[2]) then
          # This is not a new cong - drop it!
          badcong := true;
          break;
        else
          Add(newparents, i);
        fi;
      elif CongruenceTestMembershipNC(newcong, pair1[1], pair1[2]) then
        Add(newchildren, i);
      fi;
    od;
    nr := nr + 1;
    if nr > last_collected + 1999 then
      Info(InfoSemigroups, 1, "Pair ", nr, " of ", total, ": ", nrcongs,
           " congruences found so far");
      last_collected := nr;
      GASMAN("collect");
    fi;
    if not badcong then
      nrcongs := nrcongs + 1;
      congs[nrcongs] := newcong;
      for c in newchildren do
        Add(parents[c], nrcongs);
      od;
      for p in newparents do
        Add(children[p], nrcongs);
      od;
      Add(newchildren, nrcongs);  # Include loops (reflexive)
      Add(newparents, nrcongs);
      children[nrcongs] := newchildren;
      parents[nrcongs] := newparents;
    fi;
  od;

  SEMIGROUPS.OptionsRec(S).report := report;

  # We are done: make the object and return
  poset := Digraph(parents);
  SetInNeighbours(poset, children);
  SetCongruencesOfPoset(poset, congs);
  SetDigraphVertexLabels(poset, congs);
  SetUnderlyingSemigroupOfCongruencePoset(poset, S);
  SetFilterObj(poset, IsCongruencePoset);
  return poset;
end;

InstallMethod(MinimalCongruences,
"for a congruence poset",
[IsCongruencePoset],
poset -> CongruencesOfPoset(poset){Positions(InDegrees(poset), 1)});

InstallMethod(MinimalCongruences,
"for a list or collection",
[IsListOrCollection],
coll -> MinimalCongruences(PosetOfCongruences(coll)));

InstallMethod(JoinSemilatticeOfCongruences,
"for a congruence poset and a function",
[IsCongruencePoset, IsFunction],
function(poset, join_func)
  local children, parents, congs, princ_congs, nrcongs, S, report, length,
        found, ignore, start, i, j, newcong, badcong, newchildren, newparents,
        k, c, p;

  # Trivial case
  if Size(poset) = 0 then
    return poset;
  fi;

  # Extract the info
  children := InNeighboursMutableCopy(poset);
  parents := OutNeighboursMutableCopy(poset);
  congs := ShallowCopy(CongruencesOfPoset(poset));
  princ_congs := ShallowCopy(congs);
  nrcongs := Length(congs);
  S := UnderlyingSemigroupOfCongruencePoset(poset);

  # Suppress reporting
  report := SEMIGROUPS.OptionsRec(S).report;
  SEMIGROUPS.OptionsRec(S).report := false;

  # Take all the joins
  Info(InfoSemigroups, 1, "Finding joins of congruences . . .");
  length := 0;
  found := true;
  # 'ignore' is a list of congs that we don't try joining
  ignore := BlistList([1 .. nrcongs], []);
  while found do
    # There are new congs to try joining
    start := length + 1;      # New congs start here
    found := false;           # Have we found any more congs on this sweep?
    length := Length(congs);  # Remember starting position for next sweep

    for i in [start .. Length(congs)] do      # for each new cong
      for j in [1 .. Length(princ_congs)] do  # for each 1-generated cong
        newcong := join_func(congs[i], princ_congs[j]);
        badcong := false;   # Is newcong the same as another cong?
        newchildren := [];  # Children of newcong
        newparents := [];   # Parents of newcong
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
        if not badcong then
          nrcongs := nrcongs + 1;
          congs[nrcongs] := newcong;
          for c in newchildren do
            Add(parents[c], nrcongs);
          od;
          for p in newparents do
            Add(children[p], nrcongs);
          od;
          Add(newchildren, nrcongs);  # Include loops (reflexive)
          Add(newparents, nrcongs);
          children[nrcongs] := newchildren;
          parents[nrcongs] := newparents;
          ignore[nrcongs] := false;
          found := true;
        fi;
      od;
      Info(InfoSemigroups, 2, "Processed cong ", i, " of ", Length(congs),
           " (", Length(congs) - i, " remaining)");
    od;
  od;

  SEMIGROUPS.OptionsRec(S).report := report;

  # We are done: make the object and return
  poset := Digraph(parents);
  SetInNeighbours(poset, children);
  SetCongruencesOfPoset(poset, congs);
  SetDigraphVertexLabels(poset, congs);
  SetUnderlyingSemigroupOfCongruencePoset(poset, S);
  SetFilterObj(poset, IsCongruencePoset);
  return poset;
end);

InstallMethod(JoinSemilatticeOfCongruences,
"for a list or collection, and a function",
[IsListOrCollection, IsFunction],
function(coll, join_func)
  local poset;
  poset := PosetOfCongruences(coll);
  return JoinSemilatticeOfCongruences(poset, join_func);
end);

SEMIGROUPS.AddTrivialCongruence := function(poset, cong_func)
  local S, children, parents, congs, nrcongs, i;
  # Extract the info
  S := UnderlyingSemigroupOfCongruencePoset(poset);
  children := InNeighboursMutableCopy(poset);
  parents := OutNeighboursMutableCopy(poset);
  congs := ShallowCopy(CongruencesOfPoset(poset));

  # Add the trivial congruence
  nrcongs := Length(congs) + 1;
  Add(congs, cong_func(S, []), 1);
  children := Concatenation([[]], children + 1);
  parents := Concatenation([[1 .. nrcongs]], parents + 1);
  for i in [1 .. nrcongs] do
    Add(children[i], 1, 1);
  od;

  # Make the object and return
  poset := Digraph(parents);
  SetInNeighbours(poset, children);
  SetCongruencesOfPoset(poset, congs);
  SetDigraphVertexLabels(poset, congs);
  SetUnderlyingSemigroupOfCongruencePoset(poset, S);
  SetFilterObj(poset, IsCongruencePoset);
  return poset;
end;

InstallMethod(PosetOfCongruences,
"for a list or collection",
[IsListOrCollection],
function(coll)
  local congs, nrcongs, children, parents, i, ignore, j, poset;
  congs := AsList(coll);
  nrcongs := Length(congs);

  # Setup children and parents lists
  children := [];
  parents := [];
  for i in [1 .. nrcongs] do
    children[i] := Set([i]);
    parents[i] := Set([i]);
  od;

  # Find children of each cong in turn
  for i in [1 .. nrcongs] do
    # Ignore known parents
    ignore := BlistList([1 .. nrcongs], [parents[i]]);
    for j in [1 .. nrcongs] do
      if not ignore[j] and IsSubrelation(congs[i], congs[j]) then
        AddSet(children[i], j);
        AddSet(parents[j], i);
      fi;
    od;
  od;

  # We are done: make the object and return
  poset := Digraph(parents);
  SetInNeighbours(poset, children);
  SetCongruencesOfPoset(poset, congs);
  SetDigraphVertexLabels(poset, congs);
  if nrcongs > 0 then
    SetUnderlyingSemigroupOfCongruencePoset(poset, Range(congs[1]));
  fi;
  SetFilterObj(poset, IsCongruencePoset);
  return poset;
end);

InstallMethod(LatticeOfLeftCongruences,
"for a semigroup", [IsSemigroup],
S -> LatticeOfLeftCongruences(S, S));

InstallMethod(LatticeOfRightCongruences,
"for a semigroup", [IsSemigroup],
S -> LatticeOfRightCongruences(S, S));

InstallMethod(LatticeOfCongruences,
"for a semigroup", [IsSemigroup],
S -> LatticeOfCongruences(S, S));

InstallMethod(LatticeOfLeftCongruences,
"for a semigroup and a multiplicative element collection",
[IsSemigroup, IsMultiplicativeElementCollection],
function(S, restriction)
  local poset;
  poset := PosetOfPrincipalLeftCongruences(S, restriction);
  poset := JoinSemilatticeOfCongruences(poset, JoinLeftSemigroupCongruences);
  poset := SEMIGROUPS.AddTrivialCongruence(poset, LeftSemigroupCongruence);
  return poset;
end);

InstallMethod(LatticeOfRightCongruences,
"for a semigroup and a multiplicative element collection",
[IsSemigroup, IsMultiplicativeElementCollection],
function(S, restriction)
  local poset;
  poset := PosetOfPrincipalRightCongruences(S, restriction);
  poset := JoinSemilatticeOfCongruences(poset, JoinRightSemigroupCongruences);
  poset := SEMIGROUPS.AddTrivialCongruence(poset, RightSemigroupCongruence);
  return poset;
end);

InstallMethod(LatticeOfCongruences,
"for a semigroup and a multiplicative element collection",
[IsSemigroup, IsMultiplicativeElementCollection],
function(S, restriction)
  local poset;
  poset := PosetOfPrincipalCongruences(S, restriction);
  poset := JoinSemilatticeOfCongruences(poset, JoinSemigroupCongruences);
  poset := SEMIGROUPS.AddTrivialCongruence(poset, SemigroupCongruence);
  return poset;
end);

InstallMethod(LeftCongruencesOfSemigroup,
"for a semigroup", [IsSemigroup],
S -> CongruencesOfPoset(LatticeOfLeftCongruences(S)));

InstallMethod(RightCongruencesOfSemigroup,
"for a semigroup", [IsSemigroup],
S -> CongruencesOfPoset(LatticeOfRightCongruences(S)));

InstallMethod(CongruencesOfSemigroup,
"for a semigroup", [IsSemigroup],
S -> CongruencesOfPoset(LatticeOfCongruences(S)));

InstallMethod(PosetOfPrincipalLeftCongruences,
"for a semigroup", [IsSemigroup],
S -> PosetOfPrincipalLeftCongruences(S, S));

InstallMethod(PosetOfPrincipalRightCongruences,
"for a semigroup", [IsSemigroup],
S -> PosetOfPrincipalRightCongruences(S, S));

InstallMethod(PosetOfPrincipalCongruences,
"for a semigroup", [IsSemigroup],
S -> PosetOfPrincipalCongruences(S, S));

InstallMethod(PosetOfPrincipalLeftCongruences,
"for a semigroup and a multiplicative element collection",
[IsSemigroup, IsMultiplicativeElementCollection],
function(S, restriction)
  local genpairs;
  if not (IsFinite(S) and IsEnumerableSemigroupRep(S)) then
    ErrorNoReturn("Semigroups: PosetOfPrincipalLeftCongruences: usage,\n",
                  "first argument <S> must be an enumerable finite semigroup,");
  elif not IsSubset(S, restriction) then
    ErrorNoReturn("Semigroups: PosetOfPrincipalLeftCongruences: usage,\n",
                  "<restriction> must be a subset of <S>,");
  fi;
  genpairs := GeneratingPairsOfLeftSemigroupCongruence;
  return SEMIGROUPS.PrincipalXCongruencePosetNC(S,
                                                restriction,
                                                LeftSemigroupCongruence,
                                                genpairs);
end);

InstallMethod(PosetOfPrincipalRightCongruences,
"for a semigroup and a multiplicative element collection",
[IsSemigroup, IsMultiplicativeElementCollection],
function(S, restriction)
  local genpairs;
  if not (IsFinite(S) and IsEnumerableSemigroupRep(S)) then
    ErrorNoReturn("Semigroups: PosetOfPrincipalRightCongruences: usage,\n",
                  "first argument <S> must be an enumerable finite semigroup,");
  elif not IsSubset(S, restriction) then
    ErrorNoReturn("Semigroups: PosetOfPrincipalRightCongruences: usage,\n",
                  "<restriction> must be a subset of <S>,");
  fi;
  genpairs := GeneratingPairsOfRightSemigroupCongruence;
  return SEMIGROUPS.PrincipalXCongruencePosetNC(S,
                                                restriction,
                                                RightSemigroupCongruence,
                                                genpairs);
end);

InstallMethod(PosetOfPrincipalCongruences,
"for a semigroup and a multiplicative element collection",
[IsSemigroup, IsMultiplicativeElementCollection],
function(S, restriction)
  local genpairs;
  if not (IsFinite(S) and IsEnumerableSemigroupRep(S)) then
    ErrorNoReturn("Semigroups: PosetOfPrincipalCongruences: usage,\n",
                  "first argument <S> must be an enumerable finite semigroup,");
  elif not IsSubset(S, restriction) then
    ErrorNoReturn("Semigroups: PosetOfPrincipalCongruences: usage,\n",
                  "<restriction> must be a subset of <S>,");
  fi;
  genpairs := GeneratingPairsOfSemigroupCongruence;
  return SEMIGROUPS.PrincipalXCongruencePosetNC(S,
                                                restriction,
                                                SemigroupCongruence,
                                                genpairs);
end);

InstallMethod(MinimalLeftCongruencesOfSemigroup,
"for a semigroup", [IsSemigroup],
S -> MinimalLeftCongruencesOfSemigroup(S, S));

InstallMethod(MinimalRightCongruencesOfSemigroup,
"for a semigroup", [IsSemigroup],
S -> MinimalRightCongruencesOfSemigroup(S, S));

InstallMethod(MinimalCongruencesOfSemigroup,
"for a semigroup", [IsSemigroup],
S -> MinimalCongruencesOfSemigroup(S, S));

InstallMethod(MinimalLeftCongruencesOfSemigroup,
"for a semigroup and a multiplicative element collection",
[IsSemigroup, IsMultiplicativeElementCollection],
function(S, restriction)
  return MinimalCongruences(PosetOfPrincipalLeftCongruences(S, restriction));
end);

InstallMethod(MinimalRightCongruencesOfSemigroup,
"for a semigroup and a multiplicative element collection",
[IsSemigroup, IsMultiplicativeElementCollection],
function(S, restriction)
  return MinimalCongruences(PosetOfPrincipalRightCongruences(S, restriction));
end);

InstallMethod(MinimalCongruencesOfSemigroup,
"for a semigroup and a multiplicative element collection",
[IsSemigroup, IsMultiplicativeElementCollection],
function(S, restriction)
  return MinimalCongruences(PosetOfPrincipalCongruences(S, restriction));
end);

InstallMethod(PrincipalLeftCongruencesOfSemigroup,
"for a semigroup", [IsSemigroup],
S -> PrincipalLeftCongruencesOfSemigroup(S, S));

InstallMethod(PrincipalRightCongruencesOfSemigroup,
"for a semigroup", [IsSemigroup],
S -> PrincipalRightCongruencesOfSemigroup(S, S));

InstallMethod(PrincipalCongruencesOfSemigroup,
"for a semigroup", [IsSemigroup],
S -> PrincipalCongruencesOfSemigroup(S, S));

InstallMethod(PrincipalLeftCongruencesOfSemigroup,
"for a semigroup and a multiplicative element collection",
[IsSemigroup, IsMultiplicativeElementCollection],
function(S, restriction)
  return CongruencesOfPoset(PosetOfPrincipalLeftCongruences(S, restriction));
end);

InstallMethod(PrincipalRightCongruencesOfSemigroup,
"for a semigroup and a multiplicative element collection",
[IsSemigroup, IsMultiplicativeElementCollection],
function(S, restriction)
  return CongruencesOfPoset(PosetOfPrincipalRightCongruences(S, restriction));
end);

InstallMethod(PrincipalCongruencesOfSemigroup,
"for a semigroup and a multiplicative element collection",
[IsSemigroup, IsMultiplicativeElementCollection],
function(S, restriction)
  return CongruencesOfPoset(PosetOfPrincipalCongruences(S, restriction));
end);

InstallMethod(DotString,
"for a congruence poset",
[IsCongruencePoset],
function(poset)
  # Call the below function, with default options
  return DotString(poset, rec());
end);

InstallMethod(DotString,
"for a congruence poset and a record",
[IsCongruencePoset, IsRecord],
function(poset, opts)
  local nrcongs, congs, S, symbols, i, nr, in_nbs, rel, str, j, k;
  nrcongs := Size(poset);
  # Setup unbound options
  if not IsBound(opts.info) then
    opts.info := false;
  fi;
  if not IsBound(opts.numbers) then
    opts.numbers := (nrcongs < 40);
  fi;
  # If the user wants info, then change the node labels
  if opts.info = true then
    # The congruences are stored inside the poset object
    congs := CongruencesOfPoset(poset);
    S := Range(congs[1]);
    symbols := EmptyPlist(nrcongs);
    for i in [1 .. nrcongs] do
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
    symbols := List([1 .. nrcongs], String);
  fi;

  in_nbs := InNeighbours(poset);
  rel := List([1 .. nrcongs], x -> Filtered(in_nbs[x], y -> x <> y));
  str := "";

  if opts.numbers then
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
